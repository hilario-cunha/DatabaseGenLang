using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class ZonesReader : ExecuteReader<ZonesRow>
    {
        public ZonesReader(SQLiteDb db,string featureId)
            : base(db,"ZonesDal","ZonesReader","SELECT FeatureId,ZoneCode,Value From ra_Zones Where FeatureId=@FeatureId",new SQLiteParameter("@FeatureId",featureId))
        {
        }
        protected override ZonesRow Map(CustomSQLiteDataReader dr)
        {
            return new ZonesRow(dr.GetAsString("FeatureId"),dr.GetAsString("ZoneCode"),dr.GetAsString("Value"));
        }
    }
}