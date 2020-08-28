using System.Collections.Generic;
using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class ZonesInsertOrUpdate : ExecuteNonQueryInBulk<ZonesRow>
    {
        public ZonesInsertOrUpdate(SQLiteDb db,IEnumerable<ZonesRow> rows)
            : base(db,"ZonesDal","ZonesInsertOrUpdate",rows,"insert or replace into ra_Zones (FeatureId,ZoneCode,Value) values (@FeatureId,@ZoneCode,@Value)",new SQLiteParameter("@FeatureId"),new SQLiteParameter("@ZoneCode"),new SQLiteParameter("@Value"))
        {
        }
        public override void UpdateCommandParameters(SQLiteParameterCollection parameters,ZonesRow row)
        {
            parameters["@FeatureId"].Value = row.FeatureId;
            parameters["@ZoneCode"].Value = row.ZoneCode;
            parameters["@Value"].Value = row.Value;
        }
    }
}