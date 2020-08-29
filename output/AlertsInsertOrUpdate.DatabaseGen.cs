using System.Collections.Generic;
using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class AlertsInsertOrUpdate : ExecuteNonQueryInBulk<AlertsRow>
    {
        public AlertsInsertOrUpdate(SQLiteDb db,IEnumerable<AlertsRow> rows)
            : base(db,"AlertsDal","AlertsInsertOrUpdate",rows,"insert or replace into ra_Alerts (ItemId,ParentId,AlertId,Level,Name) values (@ItemId,@ParentId,@AlertId,@Level,@Name)",new SQLiteParameter("@ItemId"),new SQLiteParameter("@ParentId"),new SQLiteParameter("@AlertId"),new SQLiteParameter("@Level"),new SQLiteParameter("@Name"))
        {
        }
        public override void UpdateCommandParameters(SQLiteParameterCollection parameters,AlertsRow row)
        {
            parameters["@ItemId"].Value = row.ItemId;
            parameters["@ParentId"].Value = row.ParentId;
            parameters["@AlertId"].Value = row.AlertId;
            parameters["@Level"].Value = row.Level;
            parameters["@Name"].Value = row.Name;
        }
    }
}