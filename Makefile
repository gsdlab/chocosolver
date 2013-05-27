all:
	mvn install

javadoc:
	mvn javadoc:javadoc -Dlinksource=true
	@echo "Developer reminder: Copy the contents of target/site/apidocs to the gh-pages branch to update the online Javadocs."

clean:
	mvn clean
