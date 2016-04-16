setMethod("groupClass", "DatasetOrder", function (x) "DatasetGroup")
setMethod("groupClass", "DatasetGroup", function (x) "DatasetGroup")
setMethod("entityClass", "DatasetOrder", function (x) "CrunchDataset")
setMethod("entityClass", "DatasetGroup", function (x) "CrunchDataset")
