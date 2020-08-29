using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class AlertsReader : ExecuteReader<AlertsRow>
    {
        public AlertsReader(SQLiteDb db,string itemId,string parentId)
            : base(db,"AlertsDal","AlertsReader","SELECT ItemId,ParentId,AlertId,Level,Name From ra_Alerts Where ItemId=@ItemId,(case WHEN @ParentId IS NULL THEN (ParentId is NULL) else (ParentId = @ParentId) END)",new SQLiteParameter("@ItemId",itemId),new SQLiteParameter("@ParentId",parentId))
        {
        }
        protected override AlertsRow Map(CustomSQLiteDataReader dr)
        {
            return new AlertsRow(dr.GetAsString("ItemId"),dr.GetAsString("ParentId"),dr.GetAsString("AlertId"),dr.GetAsInt("Level"),dr.GetAsString("Name"));
        }
    }
}