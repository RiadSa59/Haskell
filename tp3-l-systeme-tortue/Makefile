.PHONY: clean

tortue: tortue.hs
	   ghc -Wall tortue.hs -o tortue 
	   LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libglut.so runghc tortue.hs
	   
           
tortue_volante: tortue_volante.hs
	   ghc -Wall tortue_volante.hs -o tortue_volante 
	   LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libglut.so runghc tortue_volante.hs


clean:
	rm -vf *.o
	rm -vf tortue
	rm -vf tortue_volante
	rm -vf *.hi

