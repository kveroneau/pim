dist: SimsDB.js SimsDB.json index.html
	mkdir dist
	cp $^ dist/

SimsDB.js: SimsDB.lpr
	pas2js -Tbrowser -Pecmascript5 -MObjFPC -Sc -O2 -l -vewnhibq -Fijs -Fu. -FUjs -FE. -o$@ -dPas2js -dPas2js -Jeutf-8 -Jirtl.js -Jc -Jm- $^

clean:
	rm -rf SimsDB.js dist
