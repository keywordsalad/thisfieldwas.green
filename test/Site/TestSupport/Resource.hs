module Site.TestSupport.Resource where

-- | Creates a spec resource dependent on a bracketed resource and runs the spec with it
--
-- Example Usage:
-- >>> around (withBracketedResource `providing` thisComponent) do
-- >>>   describe "this thing" do
providing ::
  -- | The factory providing the bracketed resource
  ((a -> m c) -> m c) ->
  -- | The factory providing the spec resource dependent on the bracketed resource
  (a -> (b -> m c) -> m c) ->
  -- | The spec requiring the dependent resource
  (b -> m c) ->
  m c
providing withResource thingForSpec theSpec = withResource (`thingForSpec` theSpec)
