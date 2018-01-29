namespace FsCheck.Experimental

open System.Runtime.CompilerServices
open System.Reflection
open System.Threading
open System
open FsCheck
open System.Collections.Generic

[<AbstractClass>]
type Setup<'Actual,'Model>() =
    ///Randomly generate the initial state of the actual object. Should still correspond to the 
    ///initial state of model object; so you should only randomly generate parameters to the instance
    ///that don't affect the model.
    ///Note:make sure that each value is truly a new instance if the commands change the state
    ///of the object. Gen.connstant in particular is a bad idea - use Gen.fresh instead.
    abstract Actual : unit -> 'Actual
    ///Initial state of model object. Must correspond to initial state of actual object.
    abstract Model : unit -> 'Model
    override __.ToString() = sprintf "Setup %s" typeof<'Actual>.Name

type IOperation =
    abstract Gets : IOperationResult -> unit
    abstract Sets : IOperationResult -> unit
    abstract Needs : ISet<IOperationResult>
    abstract Provides : ISet<IOperationResult>
    abstract ClearDependencies : unit -> unit

and IOperationResult = 
    abstract Reset : unit -> unit

//a single assignment variable
type OperationResult<'a>(?name) =
    static let mutable resultCounter = 0
    static let nextCounter() = Interlocked.Increment &resultCounter
    let name = defaultArg name "var"
    let counter = nextCounter()
    let mutable result :'a option = None

    interface IOperationResult with
        override __.Reset() = result <- None

    member __.Counter = counter
    member res.V with get (operation:IOperation) = operation.Gets res;  result.Value
                 and  set (operation:IOperation) v = match result with None -> operation.Sets res; result <- Some v | Some _ -> invalidOp "Result already set to value."
    
    override __.ToString() = sprintf "%s_%i" name counter
    override __.GetHashCode() = counter
    override __.Equals other = 
        match other with
        | :? OperationResult<'a> as oth -> oth.Counter = counter
        | _ -> false
    

///An operation describes pre and post conditions and the model for a single operation under test.
///The post-conditions are the invariants that will be checked; when these do not hold the test fails.
[<AbstractClass>]
type Operation<'Actual,'Model>() =
    let sets = HashSet<IOperationResult>()
    let gets = HashSet<IOperationResult>()
    interface IOperation with
        override __.Gets opResult = gets.Add opResult |> ignore
        override __.Sets opResult = sets.Add opResult |> ignore
        override __.Needs = upcast gets
        override __.Provides = upcast sets
        override __.ClearDependencies() = 
            sets |> Seq.iter (fun s -> s.Reset())
            gets.Clear()
            sets.Clear()
    ///Excecutes the command on the object under test, and returns a property that must hold.
    ///This property typically compares the state of the model with the state of the object after
    ///execution of the command.
    abstract Check : 'Actual * 'Model  -> Property
    ///Executes the command on the model of the object.
    abstract Run : 'Model -> 'Model
    ///Optional precondition for execution of the command. When this does not hold, the test continues
    ///but the command is not executed.
    abstract Pre : 'Model -> bool
    ///The default precondition is true.
    default __.Pre _ = true

type StopOperation<'Actual,'Model>() =
    inherit Operation<'Actual,'Model>()
    override __.Check (_, _) = true.ToProperty()
    override __.Run m = m
    override __.ToString() = "Stop"


type TearDown<'Actual>() =
    abstract Actual : 'Actual -> unit
    default __.Actual _ = ()
    override __.ToString() = ""

///Defines the initial state for actual and model object, and allows to define the generator to use
///for the next state, based on the model.
[<AbstractClass>]
type Machine<'Actual,'Model>(maxNumberOfCommands:int) =
    new() = Machine(-1)
    member __.MaxNumberOfCommands = maxNumberOfCommands

    abstract Setup : Gen<Setup<'Actual,'Model>>
    abstract TearDown : TearDown<'Actual>
    default __.TearDown = TearDown<_>()
 
    ///Generate a number of possible commands based on the current state of the model. 
    ///Preconditions are still checked, so even if a Command is returned, it is not chosen
    ///if its precondition does not hold.
    abstract Next : 'Model -> Gen<Operation<'Actual,'Model>>
    //abstract ShrinkOperations : list<Operation<'Actual,'Model>> -> seq<list<Operation<'Actual,'Model>>>
    //default __.ShrinkOperations s = Arb.Default.FsList().Shrinker s

[<StructuredFormatDisplayAttribute("{StructuredToString}")>]
type MachineRun<'Actual, 'Model> =
    { Setup         : 'Model * Setup<'Actual,'Model> 
      Operations    : list<Operation<'Actual,'Model> * 'Model>
      TearDown      : TearDown<'Actual> 
      UsedSize      : int} 
      with
    override t.ToString() =
        sprintf "%A\n%s\n%A" t.Setup (String.Join("\n", t.Operations |> Seq.map (fun (op,m) -> sprintf "%O -> %A" op m ))) t.TearDown
    member t.StructuredToString = t.ToString()

// ------------- create Machine from class definition ----------


type ObjectMachineModel = OperationResult<obj> * OperationResult<obj> * string //objectUndertest * operationResult * methodname

type New<'Actual>(ctor:ConstructorInfo, parameters:array<obj>) =
    inherit Setup<'Actual,ObjectMachineModel>()
    let paramstring = 
        parameters
        |> Seq.map (sprintf "%O") 
        |> String.concat ", "
    override __.Actual() = ctor.Invoke parameters :?> 'Actual
    override __.Model() = 
        let result = OperationResult()
        let str = sprintf "let %O = new %s(%s)" result typeof<'Actual>.Name paramstring
        (result, result, str)

type MethodCall<'Actual>(meth:MethodInfo, parameters:array<obj>) =
    inherit Operation<'Actual,ObjectMachineModel>()
    let paramstring = 
                  parameters
                  |> Seq.map (sprintf "%O") 
                  |> String.concat ", " 
    override __.Run ((objectUnderTest, _, _)) =
        let result = OperationResult()
        let str = sprintf "let %O = %O.%s(%s)" result objectUnderTest meth.Name paramstring
        objectUnderTest, result, str
    override op.Check(actual, (_, operationResult, _)) =
        let result = lazy let result = meth.Invoke(actual, parameters)
                          operationResult.V op = result 
        Prop.ofTestable result //basically just check it doesn't throw
    override __.ToString() =
        sprintf "%s(%s)" meth.Name paramstring

type DisposeCall<'Actual>() =
    inherit TearDown<'Actual>()
    override __.Actual actual = match box actual with :? IDisposable as d -> d.Dispose() | _ -> ()
    override __.ToString() = if typeof<IDisposable>.GetTypeInfo().IsAssignableFrom (typeof<'Actual>.GetTypeInfo()) then sprintf "Dispose" else "Nothing"

type ObjectMachine<'Actual>(?methodFilter:MethodInfo -> bool) = 
    inherit Machine<'Actual,ObjectMachineModel>()
    static let skipMethods = [ "GetType"; "Finalize"; "MemberwiseClone"; "Dispose"; "System-IDisposable-Dispose"] |> Set.ofList
    let methodFilter = defaultArg methodFilter (fun mi -> not <| Set.contains mi.Name skipMethods)
    let parameterGenerator (parameters:seq<ParameterInfo>) =
        parameters |> Seq.map (fun p -> Arb.getGenerator p.ParameterType) |> Gen.sequence

    let ctors = 
        typeof<'Actual>.GetTypeInfo().DeclaredConstructors
        |> Seq.map (fun ctor -> 
                        gen { let! parameters = parameterGenerator (ctor.GetParameters())
                              return New<'Actual>(ctor, List.toArray parameters) :> Setup<'Actual,ObjectMachineModel> })
        |> Gen.oneof

    let instanceMethods =
        typeof<'Actual>.GetRuntimeMethods()
        |> Seq.filter methodFilter
        |> Seq.map (fun meth -> 
                        gen { let! parameters = parameterGenerator (meth.GetParameters())
                              return MethodCall<'Actual>(meth, List.toArray parameters) :> Operation<'Actual,ObjectMachineModel> })
        |> Gen.oneof

    override __.Setup = ctors
    override __.TearDown = upcast DisposeCall<'Actual>()
    override __.Next _ = instanceMethods

module StateMachine =
    open System
    open System.ComponentModel

    [<CompiledName("Setup"); EditorBrowsable(EditorBrowsableState.Never)>]
    let setup actual model =
        { new Setup<_,_>() with
            override __.Actual() = actual()
            override __.Model() = model() }

    [<CompiledName("Setup"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let setupFunc (actual:Func<_>) (model:Func<_>) =
        { new Setup<_,_>() with
            override __.Actual() = actual.Invoke()
            override __.Model() = model.Invoke() }

    [<CompiledName("TearDown"); EditorBrowsable(EditorBrowsableState.Never)>]
    let tearDown run =
        { new TearDown<_>() with
            override __.Actual actual = run actual }

    [<CompiledName("TearDown"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let tearDownAction (run:Action<_>) =
        { new TearDown<_>() with
            override __.Actual actual = run.Invoke actual }

    [<CompiledName("Operation"); EditorBrowsable(EditorBrowsableState.Never)>]
    let operationWithPrecondition name preCondition runModel check =
        { new Operation<'Actual,'Model>() with
            override __.Run pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.Pre model = preCondition model
            override __.ToString() = name }

    [<CompiledName("Operation"); EditorBrowsable(EditorBrowsableState.Never)>]
    let operation name runModel check =
        { new Operation<'Actual,'Model>() with
            override __.Run pre = runModel pre
            override __.Check(model,actual) = check (model,actual) |> Prop.ofTestable
            override __.ToString() = name }

    [<CompiledName("Operation"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let operationProp<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Func<'Actual,_,Property>) =
        operation name runModel.Invoke (fun (a,b) -> check.Invoke(a,b))

    [<CompiledName("Operation"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let operationBool<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Func<'Actual,_,bool>) =
        operation name runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))

    [<CompiledName("Operation"); CompilerMessage("This method is not intended for use from F#.", 10001, IsHidden=true, IsError=false)>]
    let operationAction<'Actual,'Model> name (runModel:Func<'Model,_>) (check:Action<'Actual,_>) =
        operation name runModel.Invoke (fun (a,b) -> Prop.ofTestable <| check.Invoke(a,b))

    [<CompiledName("Generate")>]
    let generate (spec:Machine<'Actual,'Model>) = 
        let rec genCommandsS state size =
            gen {
                if size > 0 then
                    let nextOperation = spec.Next state
                    let! command = nextOperation |> Gen.tryWhere (fun operation -> operation.Pre state)
                    if Option.isNone command || command.Value.GetType() = typeof<StopOperation<'Actual,'Model>> then 
                        return [state],[]
                    else
                        let! states, commands = genCommandsS (command.Value.Run state) (size-1)
                        return state :: states, command.Value :: commands
                else
                    return [state],[]
            }
        Gen.sized (fun size ->
            gen { let! setup = spec.Setup
                  let initialModel = setup.Model()
                  let maxNum = spec.MaxNumberOfCommands
                  let usedSize = if maxNum < 0 then size else maxNum 
                  let! models,operations = genCommandsS initialModel usedSize
                  return { Setup = initialModel, setup
                           Operations = List.zip operations (List.tail models) //first state is actually the initial state; so drop it.
                           TearDown = spec.TearDown
                           UsedSize = usedSize }
            })

    //[<CompiledName("Shrink")>]
    //let shrink (spec:Machine<'Actual,'Model>) (run:MachineRun<_,_>) =
    //    let runModels initial (operations:seq<Operation<'Actual,'Model>>) =
    //        let addProvided (set:HashSet<_>) (op:IOperation) =
    //            set.UnionWith op.Provides
    //            set
    //        let hasNeeds (op:IOperation) (provided:HashSet<_>) =
    //            let r = provided.IsSupersetOf op.Needs
    //            r

    //        operations
    //        |> Seq.scan (fun (provided, _, Lazy model) operation -> 
    //             if hasNeeds operation provided && operation.Pre model then
    //                addProvided provided operation, Some operation, lazy operation.Run model
    //             else 
    //                provided, None, lazy model)
    //            (HashSet<IOperationResult>(), 
    //             None,
    //             lazy initial)
    //        |> Seq.choose (fun (_, op, Lazy model) -> op |> Option.map (fun op -> (op,model)))
    //        //|> Seq.distinct

    //    let chooseModels setup operations =
    //        let initialModel = fst setup
    //        let transitions = runModels initialModel operations |> Seq.toList
    //        //printf "transitions %A" transitions
    //        let ok = not <| List.isEmpty transitions
    //        if ok then 
    //            Some { run with Operations = transitions; Setup = setup } 
    //        else 
    //            None 

    //    //try to shrink the list of operations
    //    let shrinkOps =
    //        run.Operations 
    //        |> List.map fst
    //        |> spec.ShrinkOperations
    //        |> Seq.choose (chooseModels run.Setup)

    //    //try to srhink the initial setup state
    //    let shrinkSetup =
    //        Arb.toShrink spec.Setup (snd run.Setup) 
    //        |> Seq.choose (fun setup -> chooseModels (setup.Model(),setup) (List.map fst run.Operations))

    //    Seq.append shrinkOps shrinkSetup
        
    /// Check one run, i.e. create a property from a single run.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let forOne { Setup = _:'Model,setup; Operations = operations; TearDown = teardown; UsedSize = usedSize } =
            let rec run actual (operations:list<Operation<'Actual,'Model> * _>) property =
                match operations with
                | [] -> teardown.Actual actual; property
                | ((op,model)::ops) -> 
                    (op :> IOperation).ClearDependencies() //side-effect :(
                    let prop = op.Check(actual, model)
                    run actual ops (property .&. prop) //not great: should stop generating once error is found
            let prop = run (setup.Actual()) operations (Prop.ofTestable true)
            let l = operations.Length
            prop |> Prop.trivial (l = 0)
                 |> Prop.classify (l > 1 && l <= 6) "short sequences (between 1-6 commands)"
                 |> Prop.classify (l > 6) "long sequences (>6 commands)"
                 |> Prop.classify (-1 <> usedSize && l < usedSize) "aborted sequences"
                 |> Prop.classify (-1 <> usedSize && l > usedSize) "longer than used size sequences (should not occur)"
                 |> Prop.classify (-1 = usedSize) "artificial sequences"

    ///Check all generated runs, i.e. create a property from an arbitrarily generated run.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let forAll (arb:Gen<MachineRun<'Actual,'Model>>) = 
        Prop.forAll arb forOne

    ///Turn a machine specification into a property.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let toProperty (spec:Machine<'Actual,'Model>) = 
        //forAll (Arb.fromGenShrink(generate spec, shrink spec))
        forAll (generate spec)

[<AbstractClass;Sealed;Extension>]
type StateMachineExtensions =
    [<Extension>]
    static member ToProperty(specification: Machine<'Actual,'Model>) = StateMachine.toProperty specification
    [<Extension>]
    static member ToProperty(arbitraryRun:Gen<MachineRun<'Actual,'Model>>) = StateMachine.forAll arbitraryRun
    [<Extension>]
    static member ToProperty(run: MachineRun<'Actual,'Model>) = StateMachine.forOne run

