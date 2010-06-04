comp  = fsc -a
bin   = FsCheck.dll
mods  = AssemblyInfo Common Random Reflect TypeClass Gen ReflectArbitrary Arbitrary Property Commands Runner Checks 
files = $(addsuffix .fs,$(addprefix FsCheck/,$(mods)))

$(bin): $(files)
	$(comp) $(addprefix -r ,$(refs)) -o $(bin) $(files)
