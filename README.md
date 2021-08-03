# genestack-study-viewer

This program is meant to simulate the abilities of a user using the genestack to view the metadata of studies without also having access to the studies data.
Provides a searchable list of studies to access and a table to display the metadata provided by the server.
Also provides the options to search for a specific value in the metadata and returns which projects contain this data.
The rows on this table can be selected to display the related study on the study table.
Users must be connected to the sanger VPN for the API calls to be accepted.

## Running

- Copy `.env.example` to `.env`
- Add a Genestack API Token to `.env`
- Run `docker-compose up`