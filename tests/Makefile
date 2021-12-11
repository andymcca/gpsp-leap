
ARMV8PFX=/opt/buildroot-armv8el-uclibc/bin/aarch64-buildroot-linux-uclibc

all:
	gcc -o arm64gen arm64gen.c -ggdb -I../arm/
	./arm64gen > bytecode.bin
	$(ARMV8PFX)-as -o bytecoderef.o arm64gen.S
	$(ARMV8PFX)-objcopy -O binary bytecoderef.o bytecoderef.bin
	@ cmp bytecoderef.bin bytecode.bin || echo "Bytecode mismatch"
	@ cmp bytecoderef.bin bytecode.bin && echo "Test passed!"


