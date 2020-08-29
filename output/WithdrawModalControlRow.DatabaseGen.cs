namespace MRS.InStore.SDK.SQLite
{
    public partial class WithdrawModalControlRow
    {
        public WithdrawModalControlRow(string featureId,string code,bool visible)
        {
            this.FeatureId = featureId;
            this.Code = code;
            this.Visible = visible;
        }
        public string FeatureId {get; private set;}
        public string Code {get; private set;}
        public bool Visible {get; private set;}
    }
}