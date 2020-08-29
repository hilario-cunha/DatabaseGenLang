using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class WithdrawModalControlReader : ExecuteReader<WithdrawModalControlRow>
    {
        public WithdrawModalControlReader(SQLiteDb db,string featureId)
            : base(db,"WithdrawModalControlDal","WithdrawModalControlReader","SELECT FeatureId,Code,Visible From ra_WithdrawModalControl Where FeatureId=@FeatureId",new SQLiteParameter("@FeatureId",featureId))
        {
        }
        protected override WithdrawModalControlRow Map(CustomSQLiteDataReader dr)
        {
            return new WithdrawModalControlRow(dr.GetAsString("FeatureId"),dr.GetAsString("Code"),dr.GetAsBool("Visible"));
        }
    }
}