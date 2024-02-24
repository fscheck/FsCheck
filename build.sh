#!/bin/bash
set -eo pipefail

dotnet --info
dotnet tool restore
dotnet restore
dotnet fake run build.fsx "$@"
