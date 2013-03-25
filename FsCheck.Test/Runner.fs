namespace FsCheck.Test

module Runner =

    open System
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open Helpers


    type TestArbitrary1 =
        static member PositiveDouble() =
            Arb.Default.Float()
            |> Arb.mapFilter abs (fun t -> t >= 0.0)

    type TestArbitrary2 =
        static member NegativeDouble() =
            Arb.Default.Float()
            |> Arb.mapFilter (abs >> ((-) 0.0)) (fun t -> t <= 0.0)

    [<Property( Arbitrary=[| typeof<TestArbitrary2>; typeof<TestArbitrary1> |] )>]
    let ``should register Arbitrary instances from Config in last to first order``(underTest:float) =
        underTest <= 0.0
        

    type TypeToInstantiate() =
        [<Property>]
        let ``should run a property on an instance``(random:int) =
            Assert.True true

    [<Arbitrary(typeof<TestArbitrary2>)>]
    module ModuleWithArbitrary =

        [<Property>]
        let ``should use Arb instances from enclosing module``(underTest:float) =
            underTest <= 0.0

        [<Property( Arbitrary=[| typeof<TestArbitrary1> |] )>]
        let ``should use Arb instance on method preferentially``(underTest:float) =
            underTest >= 0.0

        