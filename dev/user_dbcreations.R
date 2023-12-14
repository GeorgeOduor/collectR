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



# allocatiion engine ------------------------------------------------------
allReset  <- function(db_con) {
    dbExecute(db_con,'DROP TABLE IF EXISTS "t_Repayment" CASCADE')
    dbSendQuery(db_con,'CREATE TABLE IF NOT EXISTS "t_Repayment" (
                   "id" SERIAL PRIMARY KEY,
                   "ClientID" varchar(10),
                   "AccountID" varchar(14),
                   "LoanSeries" int,
                   "Amount_Paid" numeric,
                   "Team" varchar(30),
                   "Debt_Collector" varchar(50),
                   "Channel" varchar(50),
                   "RepaymentDate" date
                 )')

    dbExecute(db_con,'DROP TABLE IF EXISTS "t_CollectionTrend" CASCADE')
    dbSendQuery(db_con,
                'CREATE TABLE "t_CollectionTrend" (
         "id" SERIAL PRIMARY KEY,
         "RepaymentDate" DATE NOT NULL,
         "Team" TEXT NOT NULL,
         "Debt_Collector" TEXT NOT NULL,
         "Channel" TEXT,
         "Amount" REAL NOT NULL,
         "RepaymentTrend" REAL,
         "UpdatedBy" TEXT NOT NULL,
         "Category" TEXT NOT NULL
         )')


    dbExecute(db_con,'DROP TABLE IF EXISTS "t_EndOfMonthCollectionTrend" CASCADE')
    dbSendQuery(db_con,
                'CREATE TABLE "t_EndOfMonthCollectionTrend"  (
         "id" SERIAL PRIMARY KEY,
          "Channel" TEXT,
          "RepaymentDate" DATE,
          "Team" VARCHAR(255),
          "Debt_Collector" VARCHAR(255),
          "Outsource_Amount" NUMERIC,
          "Outsource_Count" INTEGER,
          "Recovered_Count" INTEGER,
          "Partially_Paid_Amount" NUMERIC,
          "Fully_Paid_Amount" NUMERIC,
          "Recovered_Amount" NUMERIC,
          "UpdatedBy" TEXT NOT NULL
        );')

    dbExecute(db_con,'DROP TABLE IF EXISTS "t_DisputedAccounts" CASCADE')
    dbSendQuery(db_con,
                'CREATE TABLE "t_DisputedAccounts"  (
         "id" SERIAL PRIMARY KEY,
         "Channel" TEXT,
         "AccountID" TEXT,
         "LoanSeries" NUMERIC,
         "CreatedOn" DATE,
         "UpdatedBy" TEXT NOT NULL
        );')

    dbExecute(db_con,'DROP TABLE IF EXISTS "t_FCR" CASCADE')
    dbSendQuery(db_con,'CREATE TABLE "t_FCR" (
 "id"            SERIAL PRIMARY KEY,
  "Channel"       VARCHAR,
  "Count"  NUMERIC,
  "CollectedAmount"       NUMERIC,
  "RecoveryRate"            NUMERIC,
  "Debt_Collector"          VARCHAR(255),
  "ReportDate"          DATE
  );')

    dbExecute(db_con,'DROP TABLE IF EXISTS "t_PTPRateSummary" CASCADE')
    dbSendQuery(db_con,'CREATE TABLE "t_PTPRateSummary" (
  "id"              SERIAL PRIMARY KEY,
  "Channel"         VARCHAR,
  "Team"            VARCHAR,
  "Debt_Collector"  VARCHAR(255),
  "Count"           NUMERIC,
  "CurrentBalance"  NUMERIC,
  "PTPRate"         NUMERIC,
  "ReportDate"      DATE
  );')
    # t slippages ==============
    dbExecute(db_con,'DROP TABLE IF EXISTS "t_Slipages" CASCADE')
    dbSendQuery(db_con,'CREATE TABLE "t_Slipages" (
  "id"                SERIAL PRIMARY KEY,
  "Debt_Collector"    VARCHAR(255),
  "Channel"           VARCHAR(255),
  "Count"             NUMERIC,
  "Outstanding"       NUMERIC,
  "SlipagePropotion"  NUMERIC,
  "ReportDate"        DATE );'
    )
    # t collection time ==============
    dbExecute(db_con,'DROP TABLE IF EXISTS "t_CollectionTime" CASCADE')
    dbSendQuery(db_con,'CREATE TABLE "t_CollectionTime" (
  "id"                SERIAL PRIMARY KEY,
  "Team"              VARCHAR(255),
  "Debt_Collector"    VARCHAR(255),
  "Channel"           VARCHAR(255),
  "CollectionTime"    NUMERIC,
  "ReportDate"        DATE );'
    )


}
# dbExecute(db_con,'DROP TABLE IF EXISTS "t_CustomerFeedBack" CASCADE')
dbSendQuery(db_con,
            'CREATE TABLE IF NOT EXISTS "t_CustomerFeedBack" (
            "id" SERIAL PRIMARY KEY,
            "AccountID" VARCHAR(255),
            "LoanSeries" NUMERIC,
            "FeedBackClass" VARCHAR,
            "PTPDate" VARCHAR,
            "Occupation" VARCHAR(255),
            "ExactVerbatim" VARCHAR(255),
            "DebtCollector" VARCHAR(255),
            "Channel" VARCHAR(255),
            "FeedBackDate" VARCHAR
            );')

dbExecute(db_con,'DROP TABLE IF EXISTS "t_RecoverySMS" CASCADE')
dbSendQuery(db_con,'CREATE TABLE IF NOT EXISTS"t_RecoverySMS" (
            "id" SERIAL PRIMARY KEY,
            "AccountID" VARCHAR(255),
            "LoanSeries" NUMERIC,
            "Message" VARCHAR,
            "Destination" NUMERIC,
            "SentBy" NUMERIC,
            "SentOn" DATE
            );')

dbExecute(db_con,'DROP TABLE IF EXISTS "t_FeedbackClass" CASCADE')
dbSendQuery(db_con,'CREATE TABLE IF NOT EXISTS"t_FeedbackClass" (
            "id" SERIAL PRIMARY KEY,
            "FeedBackClass" VARCHAR(255) UNIQUE,
            "CreatedBY" VARCHAR,
            "CreatedOn" DATE
            );')


dbExecute(db_con,'DROP TABLE IF EXISTS "t_Allocation" CASCADE')
dbSendQuery(db_con,'CREATE TABLE "t_Allocation" (
 "allocation_id"            SERIAL PRIMARY KEY,
  "National_ID"             VARCHAR(255),
  "Client_Name"             VARCHAR(255),
  "MFanisi_Account"         VARCHAR(255),
  "AccountID"               VARCHAR(255),
  "Mobile_No"               VARCHAR(255),
  "Disbursement_Date"       DATE,
  "Last_Installement_Date"  DATE,
  "Outsourced_Amount"       NUMERIC,
  "Days_Overdue"            INTEGER,
  "Debt_Collector"          VARCHAR(255),
  "LoanSeries"              VARCHAR(255),
  "AllocationDate"          DATE,
  "Team"                    VARCHAR(255)
);')

dbExecute(db_con,'DROP TABLE IF EXISTS "t_ClientFeedBackSummary" CASCADE')
dbSendQuery(db_con,'CREATE TABLE "t_ClientFeedBackSummary" (
 "id" SERIAL PRIMARY KEY,
  "Team"              VARCHAR(255),
  "Debt_Collector"    VARCHAR(255),
  "Channel"           VARCHAR(255),
  "FeedBack"          VARCHAR(255),
  "Outsourced_Cases"  NUMERIC,
  "Contacted_Cases"   NUMERIC,
  "Outsourced_Amount" NUMERIC,
  "Amount_Recovered"  NUMERIC,
  "ReportDate"        DATE
  );')




