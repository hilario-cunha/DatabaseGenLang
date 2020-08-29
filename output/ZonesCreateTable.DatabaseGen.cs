using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class ZonesCreateTable : ExecuteNonQuery
    {
        public ZonesCreateTable(SQLiteDb db)
            : base(db,"ZonesDal","ZonesCreateTable","CREATE TABLE ra_Zones(FeatureId VARCHAR(50) NOT NULL,ZoneCode VARCHAR(50) NOT NULL,Value VARCHAR(1000) NULL,PRIMARY KEY(FeatureId,ZoneCode))  WITHOUT ROWID")
        {
        }
    }
}