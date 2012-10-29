all:
	mvn install
	mvn dependency:copy-dependencies

clean:
	mvn clean
