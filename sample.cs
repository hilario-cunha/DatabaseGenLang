using Tlantic.SQLite;

namespace MRS.InStore.SDK.SQLite
{
    public class ZonesCreateTable : ExecuteNonQuery
    {
        public ZonesCreateTable(SQLiteDb db)
            : base(
                db,
                "ZoneDal",
                "CreateZoneTable",
                @"CREATE TABLE ra_Zones(
                    FeatureId VARCHAR(50) NOT NULL,
                    ZoneCode VARCHAR(50) NOT NULL,
                    Value VARCHAR(1000) NULL,
                    PRIMARY KEY(`ZoneCode`,`FeatureId`)
                )"
            )
        {
        }
    }
}
