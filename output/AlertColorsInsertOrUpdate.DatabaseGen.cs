using System.Collections.Generic;
using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class AlertColorsInsertOrUpdate : ExecuteNonQueryInBulk<AlertColorsRow>
    {
        public AlertColorsInsertOrUpdate(SQLiteDb db,IEnumerable<AlertColorsRow> rows)
            : base(db,"AlertColorsDal","AlertColorsInsertOrUpdate",rows,"insert or replace into ra_AlertColors (FeatureId,RangeBegin,RangeEnd,Red,Green,Blue) values (@FeatureId,@RangeBegin,@RangeEnd,@Red,@Green,@Blue)",new SQLiteParameter("@FeatureId"),new SQLiteParameter("@RangeBegin"),new SQLiteParameter("@RangeEnd"),new SQLiteParameter("@Red"),new SQLiteParameter("@Green"),new SQLiteParameter("@Blue"))
        {
        }
        public override void UpdateCommandParameters(SQLiteParameterCollection parameters,AlertColorsRow row)
        {
            parameters["@FeatureId"].Value = row.FeatureId;
            parameters["@RangeBegin"].Value = row.RangeBegin;
            parameters["@RangeEnd"].Value = row.RangeEnd;
            parameters["@Red"].Value = row.Red;
            parameters["@Green"].Value = row.Green;
            parameters["@Blue"].Value = row.Blue;
        }
    }
}