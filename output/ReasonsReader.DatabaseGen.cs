using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class ReasonsReader : ExecuteReader<ReasonsRow>
    {
        public ReasonsReader(SQLiteDb db,string featureId)
            : base(db,"ReasonsDal","ReasonsReader","SELECT FeatureId,ReasonCode,Value From ra_Reasons Where FeatureId=@FeatureId",new SQLiteParameter("@FeatureId",featureId))
        {
        }
        protected override ReasonsRow Map(CustomSQLiteDataReader dr)
        {
            return new ReasonsRow(dr.GetAsString("FeatureId"),dr.GetAsString("ReasonCode"),dr.GetAsString("Value"));
        }
    }
}