defmodule Leex.MixProject do
  use Mix.Project

  def project do
    [
      app: :leex,
      name: "Leex",
      source_url: "https://github.com/fabsolute/leex-elixir",
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package()
    ]
  end

  def package() do
    [
      name: "leex",
      files: ~w(lib .formatter.exs mix.exs README*),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/fabsolute/leex-elixir"}
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps, do: []
end
