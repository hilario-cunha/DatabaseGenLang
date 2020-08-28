using System.Collections.Generic;
using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class ReasonsInsertOrUpdate : ExecuteNonQueryInBulk<ReasonsRow>
    {
        public ReasonsInsertOrUpdate(SQLiteDb db,IEnumerable<ReasonsRow> rows)
            : base(db,"ReasonsDal","ReasonsInsertOrUpdate",rows,"insert or replace into ra_Reasons (FeatureId,ReasonCode,Value) values (@FeatureId,@ReasonCode,@Value)",new SQLiteParameter("@FeatureId"),new SQLiteParameter("@ReasonCode"),new SQLiteParameter("@Value"))
        {
        }
        public override void UpdateCommandParameters(SQLiteParameterCollection parameters,ReasonsRow row)
        {
            parameters["@FeatureId"].Value = row.FeatureId;
            parameters["@ReasonCode"].Value = row.ReasonCode;
            parameters["@Value"].Value = row.Value;
        }
    }
}