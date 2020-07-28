function (v) 
{
    (v - mean(v, na.rm = T))/sqrt(var(v, na.rm = T))
}
