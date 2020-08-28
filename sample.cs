using System.Data.SQLite;
using Tlantic.SQLite;

namespace MRS.InStore.SDK.SQLite
{
    public class ZonesReader : ExecuteReader<ZoneRow>
    {
        public ZonesReader(SQLiteDb db, string featureId)
            : base(
                db,
                "ZoneDal",
                "GetZones",
                @"SELECT FeatureId, ZoneCode, Value FROM ra_Zones WHERE FeatureId = @featureId",
                new SQLiteParameter("@featureId", featureId)
            )
        {
        }

        protected override ZoneRow Map(CustomSQLiteDataReader dr)
        {
            return new ZoneRow(dr.GetAsString("FeatureId"), dr.GetAsString("ZoneCode"), dr.GetAsString("Value"));
        }
    }
}
