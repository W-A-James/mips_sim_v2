rout_files = addi.rout \
			addiu.rout \
			add.rout \
			andi.rout \
			and.rout \
			bgez.rout \
			bgtz.rout \
			bltz.rout \
			blez.rout \
			back_j.rout \
			j.rout \
			jr.rout \
			jal.rout \
			jalr.rout \
			bne.rout \
			div.rout \
			divu.rout \
			lui.rout \
			lw.rout \
			lb.rout \
			lh.rout \
			lbu.rout \
			lhu.rout \
			mul.rout \
			nor.rout \
			ori.rout \
			or.rout \
			sll.rout \
			sllv.rout \
			sra.rout \
			srav.rout \
			srl.rout \
			srlv.rout \
			sub.rout \
			subu.rout \
			test_back_to_back_raw_dep.rout \
			test_raw_dep2.rout \
			test_raw_dep.rout \
			xori.rout \
			xor.rout \
			mtlo_mthi.rout \
			sb.rout \
			sh.rout \
			sw.rout \
			beq.rout \
			bne.rout \
			fib.rout \
			#clo.rout\

rdata_files = addi.rdata \
			addiu.rdata \
			add.rdata \
			andi.rdata \
			and.rdata \
			bgez.rdata \
			bgtz.rdata \
			bltz.rdata \
			blez.rdata \
			bne.rdata \
			div.rdata \
			divu.rdata \
			lui.rdata \
			lw.rdata \
			lb.rdata \
			lh.rdata \
			lbu.rdata \
			lhu.rdata \
			j.rdata \
			back_j.rdata \
			jr.rdata \
			jal.rdata\
			jalr.rdata\
			mul.rdata \
			nor.rdata \
			ori.rdata \
			or.rdata \
			sll.rdata \
			sllv.rdata \
			sra.rdata \
			srav.rdata \
			srl.rdata \
			srlv.rdata \
			sub.rdata \
			subu.rdata \
			test_back_to_back_raw_dep.rdata \
			test_raw_dep2.rdata \
			test_raw_dep.rdata \
			xori.rdata \
			xor.rdata \
			mtlo_mthi.rdata \
			sb.rdata \
			sh.rdata \
			sw.rdata \
			beq.rdata \
			bne.rdata \
			fib.rdata \
			#clo.rdata\

cc = mips-img-linux-gnu-gcc
as = mips-img-linux-gnu-as
ld = mips-img-linux-gnu-ld
read_elf = mips-img-linux-gnu-readelf
as_flags = -march=r2000 -g --no-trap -msoft-float
cc_flags = -march=r2000 -mfp32 -mno-check-zero-division -mno-llsc -mcompact-branches=never -fno-delayed-branch -Og -g -c
ld_flags = -nolibc -nostdlib -nodefaultlibs -static --Ttext 0x400000 --no-omagic --architecture r2000
rout = ./compile_to_rout
objdump = mips-img-linux-gnu-objdump

all: $(rdata_files) $(rout_files)

%.rout: %.dump
	$(rout) --infile $< --outfile $@

%.rdata: %.data
	$(rout) --infile $< --outfile $@

%.o: %.S
	$(as) $(as_flags) $< -o $@

%.l: %.o
	$(ld) $(ld_flags) $< -o $@

%.dump: %.l
	$(objdump) -s -z --section=.text $< > $@

%.data: %.l
	./link.sh $< $@

.PHONY: clean
clean:
	rm -f *.o *.dump *.data *.l *.rdata *.rout 
