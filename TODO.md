# To do

- Add code to test ssh connection before attempting backup. This is to
  handle the situation where the connection exists but is poor. (One
  possibility: `ssh -q arthur@do.arthur.li exit`)
- Add code to handle command failure.
- Set up tests.
- Borg does not print stats??

- Convert unmeteredConnNames to lower case when reading config file
