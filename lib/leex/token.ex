defmodule Leex.Token do
  defstruct type: :token, value: nil, push_back: nil

  def skip_token(push_back \\ nil) do
    %__MODULE__{type: :skip_token, push_back: push_back}
  end

  def token(selection, push_back \\ nil) do
    %__MODULE__{type: :token, value: selection, push_back: push_back}
  end

  def end_token(selection, push_back \\ nil) do
    %__MODULE__{type: :end_token, value: selection, push_back: push_back}
  end
end
