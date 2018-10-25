import Control.Applicative (empty)
import Control.Lens (element, preview)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
	(StateT, execStateT, get, modify)
import Data.Bool (bool)
import Data.Function ((&))

data Shape = Opened | Closed deriving (Eq, Show)

data Style = Round | Square | Angle | Curly deriving (Eq, Show)

data Symbol = Bracket Style Shape | Character deriving Eq

instance Show Symbol where
	show Character = "_"
	show (Bracket Round Opened) = "("
	show (Bracket Round Closed) = ")"
	show (Bracket Square Opened) = "["
	show (Bracket Square Closed) = "]"
	show (Bracket Angle Opened) = "<"
	show (Bracket Angle Closed) = ">"
	show (Bracket Curly Opened) = "{"
	show (Bracket Curly Closed) = "}"

recognize :: Char -> Symbol
recognize '(' = Bracket Round Opened
recognize ')' = Bracket Round Closed
recognize '[' = Bracket Square Opened
recognize ']' = Bracket Square Closed
recognize '<' = Bracket Angle Opened
recognize '>' = Bracket Angle Closed
recognize '{' = Bracket Curly Opened
recognize '}' = Bracket Curly Closed
recognize char = Character

type Gotcha = (Int, Style, Shape)

data Stumble
	= Deadend Int Style -- Closed bracket without opened one
	| Logjam Int Style -- Opened bracket without closed one
	| Mismatch Int Style Int Style -- Closing bracket doesn't match opened one

instance Show Stumble where
	show (Deadend position closed) = "Unexpectedly closed: "
		<> show (Bracket closed Closed) <> " at "
		<> show position <> " position"
	show (Logjam position closed) = "Unexpectedly opened: "
		<> show (Bracket closed Opened) <> " at "
		<> show position <> " position"
	show (Mismatch n opened m closed) = "Mismatching brackets: "
		<> show (Bracket opened Opened) <> " at " <> show n <> " and "
		<> show (Bracket closed Closed) <> " at " <> show m

handle :: (Int, Symbol) -> StateT [Gotcha] (Either Stumble) ()
handle (_, Character) = pure ()
handle (position, Bracket style Opened) = modify ((:) (position, style, Opened))
handle (position, Bracket style Closed) = preview (element 0) <$> get >>=
	maybe (lift . Left $ Deadend position style) matching where

		matching :: Gotcha -> StateT [Gotcha] (Either Stumble) ()
		matching (p, st, Closed) = modify ((:) (position, style, Closed))
		matching (p, st, Opened) = style == st & bool
			(lift . Left $ Mismatch p st position style) (modify tail)

completeness :: String -> Either Stumble ()
completeness code = (flip execStateT empty . traverse (handle . (<$>) recognize) $ zip [0..] code)
	& either Left (maybe (Right ()) (Left . logjam) . preview (element 0)) where

	logjam :: (Int, Style, Shape) -> Stumble
	logjam (position, style, _) = Logjam position style

example :: String
example = "()[]}" -- Unexpectedly closed: } at 4 position
-- example = "([](){([])})" -- All brackets converged!
-- example = "foo(bar[i);" -- Mismatching brackets: [ at 7 and ) at 9
-- example = "{{[()]]" -- Mismatching brackets: { at 1 and ] at 6

main = completeness example & either show (const "All brackets converged!") & print
