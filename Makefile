Main: Main.hs Application.hs Database.hs Routes.hs PathHelpers.hs
	ghc -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	../route-generator/dist/build/routeGenerator/routeGenerator -r -m Application -n 2 $^ > $@

PathHelpers.hs: routes
	../route-generator/dist/build/routeGenerator/routeGenerator -p -n 2 $^ > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main
