# the build date that chocosolver --version prints: the day of SOURCE_DATE_EPOCH if it is set,
# e.g. to the release date, otherwise the day of the build (solver/pom.xml)
BUILD_DATE := $(if $(SOURCE_DATE_EPOCH),-Dbuild.date=$(shell date -u -d @$(SOURCE_DATE_EPOCH) +%F))

all:
	mvn install $(BUILD_DATE)

javadoc:
	mvn javadoc:javadoc -Dlinksource=true
	@echo "Developer reminder: Copy the contents of target/site/apidocs to the gh-pages branch to update the online Javadocs."

jacoco:
	mvn org.jacoco:jacoco-maven-plugin:prepare-agent test org.jacoco:jacoco-maven-plugin:report
	@echo "Developer note: firefox target/site/jacoco/index.html"

clean:
	mvn clean

install:
	mkdir -p $(to)
	cp -f README.md $(to)/chocosolver-README.md
	cp -f solver/target/chocosolver-0.6.0-jar-with-dependencies.jar $(to)/chocosolver.jar
	cp -f lsp/target/clafer-language-server-0.6.0-jar-with-dependencies.jar $(to)/clafer-ls.jar
	cp -f lsp/clafer-ls $(to)/clafer-ls
	cp -f frontend/target/clafer-frontend-0.6.0-jar-with-dependencies.jar $(to)/claferc.jar
	cp -f frontend/claferc $(to)/claferc

snapshotInstall:
	mkdir -p $(to)
	cp -f README.md $(to)/chocosolver-README.md
	cp -f solver/target/chocosolver-0.6.0-SNAPSHOT-jar-with-dependencies.jar $(to)/chocosolver.jar
