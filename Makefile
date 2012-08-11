Main: Main.hs Application.hs Routes.hs
	ghc -Wall -fno-warn-name-shadowing $^

Routes.hs: routes
	../route-generator/dist/build/routeGenerator/routeGenerator $^ Application 1 > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main
