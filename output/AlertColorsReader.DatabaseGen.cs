using System.Data.SQLite;
using Tlantic.SQLite;
namespace MRS.InStore.SDK.SQLite
{
    public class AlertColorsReader : ExecuteReader<AlertColorsRow>
    {
        public AlertColorsReader(SQLiteDb db,string featureId)
            : base(db,"AlertColorsDal","AlertColorsReader","SELECT FeatureId,RangeBegin,RangeEnd,Red,Green,Blue From ra_AlertColors Where FeatureId=@FeatureId",new SQLiteParameter("@FeatureId",featureId))
        {
        }
        protected override AlertColorsRow Map(CustomSQLiteDataReader dr)
        {
            return new AlertColorsRow(dr.GetAsString("FeatureId"),dr.GetAsInt("RangeBegin"),dr.GetAsInt("RangeEnd"),dr.GetAsInt("Red"),dr.GetAsInt("Green"),dr.GetAsInt("Blue"));
        }
    }
}