'CREATE TABLE "t_GroupPermissions"(
    "id" serial PRIMARY KEY,
    "group_id" BIGINT NOT NULL,
    "permission_id" BIGINT NOT NULL,
    "createdon" DATE NOT NULL DEFAULT CURRENT_TIMESTAMP );' %>%
dbExecute(db_con,.)

'CREATE TABLE "t_Userpermissions"(
    "id" serial PRIMARY KEY,
    "permission_id" BIGINT NOT NULL,
    "user_id" BIGINT NOT NULL);' %>%
    dbExecute(db_con,.)

'CREATE TABLE "t_Permissions"(
    "id" serial PRIMARY KEY,
    "permission_name" VARCHAR(255) NOT NULL,
    "permission_type" VARCHAR(255) NOT NULL,
    "created_by" VARCHAR(255) NOT NULL,
    "createdon" DATE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "modified_by" VARCHAR(255) ,
    "modified_on" DATE
);' %>%
    dbExecute(db_con,.)

'CREATE TABLE "t_Users"(
    "id" serial PRIMARY KEY,
    "first_name" VARCHAR(255) NOT NULL,
    "last_name" VARCHAR(255) NOT NULL,
     "username" varchar (50) NOT NULL UNIQUE,
    "email" VARCHAR(255) NOT NULL UNIQUE,
    "start" DATE NOT NULL,
    "expire" DATE NULL,
    "usergroup" BIGINT NOT NULL,
    "password" VARCHAR(255) NOT NULL);' %>%
    dbExecute(db_con,.)

'CREATE TABLE "t_UserGroups"(
    "id" serial PRIMARY KEY,
    "name" VARCHAR(255) NULL,
    "createdon" DATE NOT NULL DEFAULT CURRENT_TIMESTAMP
);' %>%
    dbExecute(db_con,.)

'ALTER TABLE "t_GroupPermissions"
     ADD CONSTRAINT "t_grouppermissions_group_id_foreign"
     FOREIGN KEY("group_id")
     REFERENCES "t_UserGroups"("id")
     ON DELETE CASCADE;
' %>%
dbExecute(db_con,.)


'ALTER TABLE "t_Users"
    ADD CONSTRAINT "t_users_usergroup_foreign"
    FOREIGN KEY("usergroup")
    REFERENCES "t_UserGroups"("id")
    ON DELETE CASCADE;
' %>%
    dbExecute(db_con,.)

# 'ALTER TABLE "t_GroupPermissions" DROP CONSTRAINT "t_users_usergroup_foreign";'  %>% dbExecute(db_con,.)

'ALTER TABLE
    "t_GroupPermissions" ADD CONSTRAINT "t_grouppermissions_permission_id_foreign"
    FOREIGN KEY("permission_id") REFERENCES "t_Permissions"("id")
    ON DELETE CASCADE;
' %>%    dbExecute(db_con,.)

# 'ALTER TABLE "t_Userpermissions" DROP CONSTRAINT "t_userpermissions_permission_id_foreign";'  %>%
#     dbExecute(db_con,.)


'ALTER TABLE
    "t_Userpermissions" ADD CONSTRAINT "t_userpermissions_permission_id_foreign"
    FOREIGN KEY("permission_id") REFERENCES
    "t_Permissions"("id") ON DELETE CASCADE;
'%>%
    dbExecute(db_con,.)


'ALTER TABLE
    "t_Userpermissions" ADD CONSTRAINT "t_userpermissions_user_id_foreign" FOREIGN KEY("user_id") REFERENCES "t_Users"("id") ON DELETE CASCADE;
'%>%
    dbExecute(db_con,.)

