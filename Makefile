Main: Main.hs Application.hs Database.hs Routes.hs PathHelpers.hs MustacheTemplates.hs
	ghc -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	../route-generator/dist/build/routeGenerator/routeGenerator -r -m Application -n 2 $^ > $@

PathHelpers.hs: routes
	../route-generator/dist/build/routeGenerator/routeGenerator -p -n 2 $^ > $@

MustacheTemplates.hs: Records.hs view/rps.mustache view/error.mustache view/email.mustache
	../mustache2hs/mustache2hs -m Records.hs view/error.mustache ErrorMessage view/rps.mustache GameContext view/email.mustache GameContext > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main Routes.hs PathHelpers.hs MustacheTemplates.hs
