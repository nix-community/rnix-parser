{ value ? null, life }:
{
  x = if value != null
        then if value <= 5
          then value
          else 5
        else if !life
          then 1337
          else 42;
}
