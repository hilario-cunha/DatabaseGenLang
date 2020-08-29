namespace MRS.InStore.SDK.SQLite
{
    public partial class AlertColorsRow
    {
        public AlertColorsRow(string featureId,int rangeBegin,int rangeEnd,int red,int green,int blue)
        {
            this.FeatureId = featureId;
            this.RangeBegin = rangeBegin;
            this.RangeEnd = rangeEnd;
            this.Red = red;
            this.Green = green;
            this.Blue = blue;
        }
        public string FeatureId {get; private set;}
        public int RangeBegin {get; private set;}
        public int RangeEnd {get; private set;}
        public int Red {get; private set;}
        public int Green {get; private set;}
        public int Blue {get; private set;}
    }
}