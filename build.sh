#!/bin/bash
set -eo pipefail
dotnet tool restore
dotnet fake run build.fsx $@
