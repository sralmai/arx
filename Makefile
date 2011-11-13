GHC=ghc -outputdir ./tmp --make -O2


arx: arx.hs
	$(GHC) ./arx.hs -o arx

arxprof: arx
	$(GHC) -rtsopts -prof -auto-all ./arx.hs -o arxprof -osuf p_o

clean:
	rm -rf ./tmp ./arx
