namespace LITEQ

type IStore = 
    /// <summary>Returns a list of all classes with their label and comment</summary>
    abstract member Classes: (string*string*string) list
    /// <summary>Returns a list of all properties with their label, comment, domain and range</summary>
    abstract member Properties: (string*string*string*string*string) list
    /// <summary>Takes a type URI and returns a list containing property URI, label, comment and range</summary>
    /// <param name=typeUri>The URI of the type</param>
    abstract member PropertiesForType: string -> (string*string*string*string) list
    /// <summary>Takes a class URI and returns a list containing class URIs and labels</summary>
    /// <param name="typeUri">The URI of the type</param >
    abstract member SubclassesForType: string -> (string*string) list
    /// <summary>Takes a property URI and returns the range URI</summary>
    /// <param name=propertyUri>The URI of the property</param>
    abstract member RangeForProperty: string -> string