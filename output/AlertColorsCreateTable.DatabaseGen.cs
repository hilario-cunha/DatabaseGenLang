using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class AlertColorsCreateTable : ExecuteNonQuery
    {
        public AlertColorsCreateTable(SQLiteDb db)
            : base(db,"AlertColorsDal","AlertColorsCreateTable","CREATE TABLE ra_AlertColors(FeatureId VARCHAR(50) NOT NULL,RangeBegin INT NOT NULL,RangeEnd INT NOT NULL,Red INT NOT NULL,Green INT NOT NULL,Blue INT NOT NULL,PRIMARY KEY(FeatureId,RangeBegin,RangeEnd)) WITHOUT ROWID")
        {
        }
    }
}