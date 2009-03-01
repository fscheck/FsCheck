comp  = fsc -a
refs  = FSharp.PowerPack.dll
bin   = FsCheck.dll
mods  = Common Random Reflect TypeClass Generator Functions ReflectArbitrary Arbitrary Property Commands Runner Checks 
files = $(addsuffix .fs,$(addprefix FsCheck/,$(mods)))

$(bin): $(files)
	$(comp) $(addprefix -r ,$(refs)) -o $(bin) $(files)
