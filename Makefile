SForth: SForth.dmp system.fs input.fs build.sh
	./build.sh


.PHONY: run
run: SForth
	./SForth

.PHONY: dbg
dbg: SForth
	./SForth | xxd -u -g8 -o 0x0fffffe0 -R always

.PHONY: reg
reg: SForth
	./SForth | xxd -u -g8 -e -R always

.PHONY: clean
clean:
	rm -f SForth SForth0
