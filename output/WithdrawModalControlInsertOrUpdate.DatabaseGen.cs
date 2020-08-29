using System.Collections.Generic;
using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class WithdrawModalControlInsertOrUpdate : ExecuteNonQueryInBulk<WithdrawModalControlRow>
    {
        public WithdrawModalControlInsertOrUpdate(SQLiteDb db,IEnumerable<WithdrawModalControlRow> rows)
            : base(db,"WithdrawModalControlDal","WithdrawModalControlInsertOrUpdate",rows,"insert or replace into ra_WithdrawModalControl (FeatureId,Code,Visible) values (@FeatureId,@Code,@Visible)",new SQLiteParameter("@FeatureId"),new SQLiteParameter("@Code"),new SQLiteParameter("@Visible"))
        {
        }
        public override void UpdateCommandParameters(SQLiteParameterCollection parameters,WithdrawModalControlRow row)
        {
            parameters["@FeatureId"].Value = row.FeatureId;
            parameters["@Code"].Value = row.Code;
            parameters["@Visible"].Value = row.Visible;
        }
    }
}