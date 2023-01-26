defmodule Leex.Options do
  # % Include paths (list of absolute directory names).
  defstruct includes: [],
            # % Directory for result (absolute path).
            outdir: ".",
            # Type of output file.
            output_type: nil,
            defines: [],
            warning: 1,
            verbose: false,
            optimize: 999,
            specific: [],
            outfile: "",
            cwd: nil

  @type t :: %__MODULE__{
          includes: list(:file.filename()),
          outdir: :file.filename(),
          output_type: atom() | nil,
          defines: list(atom() | keyword()),
          warning: non_neg_integer(),
          verbose: boolean(),
          optimize: non_neg_integer(),
          specific: Keyword.t(),
          outfile: :file.filename(),
          cwd: :file.filename()
        }
end
