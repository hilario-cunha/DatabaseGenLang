namespace MRS.InStore.SDK.SQLite
{
    public partial class ReasonsRow
    {
        public ReasonsRow(string featureId,string reasonCode,string value)
        {
            this.FeatureId = featureId;
            this.ReasonCode = reasonCode;
            this.Value = value;
        }
        public string FeatureId {get; private set;}
        public string ReasonCode {get; private set;}
        public string Value {get; private set;}
    }
}