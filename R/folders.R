
folderFunction <- function(root){
    n = normalizePath(root)
    f = function(fp){
        file.path(n, fp)
    }
    f
}
