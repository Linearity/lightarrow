# Lightarrow

Lightarrow is a library for building applications with Bear River.

Some of its definitions are useful on their own, but many are type classes
and type families without instances.  Lightarrow should be used with another
library that provides such instances, such as [FairyBow][fairybow].  This
way an application can refer to Lightarrow definitions without being tied
to a particular implementation.



[fairybow]: https://github.com/Linearity/fairybow/
