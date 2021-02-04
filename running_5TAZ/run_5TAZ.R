# Running 5TAZ example

ex <- openModel('VERSPM_5TAZ')
ex$run()


ex$groups
ex$status
ex$fields
write.csv(ex$fields, file = 'field_list.csv')
ex$fields <- ex$list(pattern = 'Dvmt')
ex$extract()
