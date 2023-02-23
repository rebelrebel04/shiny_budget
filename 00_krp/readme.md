# DB Setup

This app uses a sqlite db hosted locally (this db service ships with OSX). The setup & maintenance of the db is split into two parts:

## Initializing the db

db_setup.R will read from the gsheet here []. This gsheet is the source of truth for all tables _other than fact_rules_. The db schema is meant to be fully normalized, so that all `selectInput` elements are restricted to the unique set of options available for that dimension in the database (such as institutions, categories, etc.).

## Updating the db

For all dim_ tables, update the gsheet directly & then rerun db_setup.R. This will overwrite these tables in the local sqlite db.

For fact_transactions, this table persists only in the local sqlite db, and is meant to be appended to with new exports of transaction data.
