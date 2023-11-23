64tass -a ./src/var-counter.asm -l ./target/var-counter.lbl -L ./target/var-counter.lst -o ./target/var-counter

c1541 -format "var counter,sh" d64 ./target/var-counter.d64
c1541 -attach ./target/var-counter.d64 -write ./target/var-counter var-counter
