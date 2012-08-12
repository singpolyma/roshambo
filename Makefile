Main: Main.hs Application.hs Database.hs Routes.hs
	ghc -Wall -fno-warn-name-shadowing $^

Routes.hs: routes
	../route-generator/dist/build/routeGenerator/routeGenerator $^ Application 2 > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main
