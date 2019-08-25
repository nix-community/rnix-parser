{ value ? null, life }:
{
  x = if value != null
        then if value <= 5
          then (assert (value >= 0); value)
          else 5
        else if !life
          then 1337
          else 42;

  enabled = false;
  value = null;
  valid = assert enabled -> value != null; true;
}
