# library(mbanalytics)
# # users table -------------------------------------------------------------
# db_con
# dbExecute(db_con,'DROP TABLE IF EXISTS "t_Users" CASCADE')
# 'CREATE TABLE IF NOT EXISTS "t_Users" (
#     id serial PRIMARY KEY,
#     first_name varchar (50) NOT NULL,
#     last_name varchar (50) NOT NULL,
#     username varchar (50) NOT NULL UNIQUE,
#     email varchar (50) NOT NULL UNIQUE,
#     start DATE NOT NULL,
#     expire DATE NOT NULL,
#     admin bool NOT NULL,
#     usergroup numeric,
#     password varchar (300)
# );' %>%
#     dbExecute(db_con,.)
# # dbListTables(con_postgre) %>% map(~dbSendQuery(con_postgre,glue('DROP TABLE IF EXISTS "{.}"')))
#
# tibble(
#     first_name = "George",
#     last_name = "Oduor",
#     username = "goduor",
#     email = "george.oduor@maishabank.com",
#     start = Sys.Date(),
#     expire = Sys.Date() + 90,
#     password = "1",
#     usergroup = 1
# ) %>% dbWriteTable(db_con,
#                    name = "t_Users",
#                    value = .,
#                    append = T)
# tbl(db_con,'t_Users')
#
# # dca table -------------------------------------------------------------
# dbExecute(db_con,'DROP TABLE IF EXISTS "t_Agents" CASCADE')
# 'CREATE TABLE IF NOT EXISTS "t_Agents" (
#     id serial PRIMARY KEY,
#     name varchar (50) NOT NULL UNIQUE,
#     start DATE NOT NULL,
#     expire DATE
# );' %>%
#     dbExecute(db_con,.)
# # dbListTables(con_postgre) %>% map(~dbSendQuery(con_postgre,glue('DROP TABLE IF EXISTS "{.}"')))
#
# tibble(
#     name = "SaniExtra",
#     start = Sys.Date(),
#     ) %>% dbWriteTable(db_con,
#                    name = "t_Agents",
#                    value = .,
#                    append = T)
# tbl(db_con,'t_Agents')
#
# # dca contacts table -------------------------------------------------------------
# dbExecute(db_con,'DROP TABLE IF EXISTS "t_Agent_Contacts" CASCADE')
#
# 'CREATE TABLE IF NOT EXISTS "t_Agent_Contacts" (
#     id serial PRIMARY KEY,
#     agent_id bigint NOT NULL,
#     name varchar (100) NOT NULL,
#     mobilenumber varchar(12),
#     emailaddress varchar(50) NOT NULL UNIQUE,
#     ismain_contact BOOLEAN NOT NULL,
#     created_on DATE NOT NULL
# );' %>%
#     dbExecute(db_con,.)
# # ,
# # FOREIGN KEY("agent_id"),
# # REFERENCES "t_Agents"("id")
# 'ALTER TABLE "t_Agent_Contacts"
# ADD CONSTRAINT "t_Agent_Contacts_id_foreign"
# FOREIGN KEY("agent_id")
# REFERENCES "t_Agents"("id")
# ON DELETE CASCADE;
# '%>%
#     dbExecute(db_con,.)
#
# # dbListTables(con_postgre) %>% map(~dbSendQuery(con_postgre,glue('DROP TABLE IF EXISTS "{.}"')))
#
# tibble(
#     name = "SaniExtra",
#     start = Sys.Date(),
# ) %>% dbWriteTable(db_con,
#                    name = "t_Agents",
#                    value = .,
#                    append = T)
# tbl(db_con,'t_Agents')
# # groups table ------------------------------------------------------------
# dbExecute(db_con,'DROP TABLE IF EXISTS "t_UserGroups"')
# 'CREATE TABLE IF NOT EXISTS "t_UserGroups" (
#     id serial PRIMARY KEY,
#     name varchar (50) NOT NULL UNIQUE,
#     createdon DATE NOT NULL DEFAULT CURRENT_TIMESTAMP
# );' %>%
#     dbExecute(db_con,.)
#
#
# # permisions --------------------------------------------------------------
#
# dbExecute(db_con,'DROP TABLE IF EXISTS "t_Permissions"')
# 'CREATE TABLE IF NOT EXISTS "t_Permissions" (
#     id serial PRIMARY KEY,
#     permission_name varchar (50) NOT NULL UNIQUE,
#     permission_type varchar (50) NOT NULL,
#     created_by varchar(60) NOT NULL,
#     createdon TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
#     modified_by varchar(60),
#     modified_on TIMESTAMP
# );' %>%
#     dbExecute(db_con,.)
#
# tibble(
#     permission_name = "Add New Agent",
#     permission_type = "Application Access",
#     created_by = 'George') %>%
#     dbWriteTable(db_con,
#                  name = "t_Permissions",
#                  value = .,
#                  append = T)
# tbl(db_con,'t_Permissions')
# dbListTables(db_con)
# # GroupPermisions --------------------------------------------------------------
# dbExecute(db_con,'DROP TABLE IF EXISTS "t_GroupPermissions"')
# 'CREATE TABLE IF NOT EXISTS "t_GroupPermissions" (
#     id serial PRIMARY KEY,
#     group_id NUMERIC NOT NULL,
#     permission_id NUMERIC NOT NULL,
#     createdon DATE NOT NULL
# );' %>%
#     dbExecute(db_con,.)
#
# tibble(
#     permission_name = "Add New Agent",
#     createdon = Sys.Date()
# ) %>%
#     dbWriteTable(db_con,
#                  name = "t_Permissions",
#                  value = .,
#                  append = T)
# tbl(db_con,'t_Permissions')
# dbListTables(db_con)
