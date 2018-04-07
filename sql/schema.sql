BEGIN;
SET client_min_messages = 'warning';
CREATE SCHEMA IF NOT EXISTS docgraph;
SET SEARCH_PATH=docgraph;

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Start of table definitions
CREATE TABLE documents (
  document_id SERIAL PRIMARY KEY,
  title       TEXT   NOT NULL,
  author      TEXT   NOT NULL,
  reference   TEXT   NOT NULL,
  version     TEXT   NOT NULL,
  keywords    TEXT   NOT NULL,
  url	      TEXT   NULL
);

-- projects table
CREATE TABLE projects (
   reference  TEXT   PRIMARY KEY,
   name       TEXT   NOT NULL
);

-- Creating the `docgraph` user with the required permissions.
-- Must happen after all tables, views, etc. have been created.
DROP ROLE IF EXISTS docgraph;
CREATE ROLE docgraph WITH LOGIN ENCRYPTED PASSWORD 'docgraph';
ALTER ROLE docgraph SET search_path TO docgraph;
GRANT USAGE ON SCHEMA docgraph TO docgraph;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA docgraph TO docgraph;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA docgraph TO docgraph;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA docgraph TO docgraph;

COMMIT;
