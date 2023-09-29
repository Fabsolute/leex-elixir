defmodule Leex.Token do
  defstruct type: :token, value: nil, push_back: nil

  @type push_back :: String.t() | nil
  @type token :: %__MODULE__{
          type: :token,
          value: any,
          push_back: push_back
        }
  @type end_token :: %__MODULE__{
          type: :end_token,
          value: any,
          push_back: push_back
        }
  @type skip_token :: %__MODULE__{
          type: :skip_token,
          push_back: push_back
        }

  @type t ::
          token
          | end_token
          | skip_token

  @spec skip_token(push_back) :: skip_token
  def skip_token(push_back \\ nil) do
    %__MODULE__{type: :skip_token, push_back: push_back}
  end

  @spec token(any, push_back) :: token
  def token(selection, push_back \\ nil) do
    %__MODULE__{type: :token, value: selection, push_back: push_back}
  end

  @spec end_token(any, push_back) :: end_token
  def end_token(selection, push_back \\ nil) do
    %__MODULE__{type: :end_token, value: selection, push_back: push_back}
  end
end
