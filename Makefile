Main: Main.hs Application.hs Routes.hs
	ghc $^

Routes.hs: routes
	../route-generator/dist/build/routeGenerator/routeGenerator $^ Application 1 > $@
