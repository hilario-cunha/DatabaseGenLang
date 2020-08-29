using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class AlertsCreateTable : ExecuteNonQuery
    {
        public AlertsCreateTable(SQLiteDb db)
            : base(db,"AlertsDal","AlertsCreateTable","CREATE TABLE ra_Alerts(ItemId VARCHAR(250) NOT NULL,ParentId VARCHAR(250) NULL,AlertId VARCHAR(250) NOT NULL,Level INT NOT NULL,Name VARCHAR(250) NULL,PRIMARY KEY(ItemId,ParentId,AlertId)) WITHOUT ROWID")
        {
        }
    }
}