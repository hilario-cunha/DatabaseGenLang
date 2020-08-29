using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class WithdrawModalControlCreateTable : ExecuteNonQuery
    {
        public WithdrawModalControlCreateTable(SQLiteDb db)
            : base(db,"WithdrawModalControlDal","WithdrawModalControlCreateTable","CREATE TABLE ra_WithdrawModalControl(FeatureId VARCHAR(50) NOT NULL,Code VARCHAR(50) NOT NULL,Visible BIT NOT NULL,PRIMARY KEY(FeatureId,Code)) WITHOUT ROWID")
        {
        }
    }
}