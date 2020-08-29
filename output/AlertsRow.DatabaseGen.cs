namespace MRS.InStore.SDK.SQLite
{
    public partial class AlertsRow
    {
        public AlertsRow(string itemId,string parentId,string alertId,int level,string name)
        {
            this.ItemId = itemId;
            this.ParentId = parentId;
            this.AlertId = alertId;
            this.Level = level;
            this.Name = name;
        }
        public string ItemId {get; private set;}
        public string ParentId {get; private set;}
        public string AlertId {get; private set;}
        public int Level {get; private set;}
        public string Name {get; private set;}
    }
}