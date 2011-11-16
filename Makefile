GHC=ghc -outputdir ./tmp --make -O2
GHCPROF=$(GHC) -rtsopts -prof -auto-all -osuf p_o


arx: arx.hs
	$(GHC) ./arx.hs -o arx

arxprof: arx
	$(GHCPROF) ./arx.hs -o arxprof

rebuild: Rebuild.hs
	$(GHC) -main-is Rebuild ./Rebuild.hs -o rebuild

rebuildprof: rebuild
	$(GHCPROF) -main-is Rebuild ./Rebuild.hs -o rebuildprof

clean:
	rm -rf ./tmp ./arx ./arxprof ./rebuild ./rebuildprof
