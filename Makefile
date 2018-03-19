-include envfile

database:
	docker run -d -p $(PGPORT):$(PGPORT) -e POSTGRES_USER=$(PGUSER) -e POSTGRES_PASSWORD=$(PGPASS) --name docgraph-postgres postgres:10.3

schema:
	dropdb --if-exists docgraph
	createdb docgraph
	psql --file=sql/schema.sql

psql:
	PGUSER=docgraph PGPASSWORD=docgraph psql
