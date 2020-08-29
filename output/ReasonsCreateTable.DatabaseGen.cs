using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class ReasonsCreateTable : ExecuteNonQuery
    {
        public ReasonsCreateTable(SQLiteDb db)
            : base(db,"ReasonsDal","ReasonsCreateTable","CREATE TABLE ra_Reasons(FeatureId VARCHAR(50) NOT NULL,ReasonCode VARCHAR(50) NOT NULL,Value VARCHAR(1000) NULL,PRIMARY KEY(FeatureId,ReasonCode)) WITHOUT ROWID")
        {
        }
    }
}