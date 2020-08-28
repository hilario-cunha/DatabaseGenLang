namespace MRS.InStore.SDK.SQLite
{
    public partial class ZonesRow
    {
        public ZonesRow(string featureId,string zoneCode,string value)
        {
            this.FeatureId = featureId;
            this.ZoneCode = zoneCode;
            this.Value = value;
        }
        public string FeatureId {get; private set;}
        public string ZoneCode {get; private set;}
        public string Value {get; private set;}
    }
}